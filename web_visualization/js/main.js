/**
 * Main JavaScript for Elephant Movement Analysis Platform
 * Handles navigation, animations, and interactive elements
 */

// ===================================
// INITIALIZATION
// ===================================
document.addEventListener('DOMContentLoaded', () => {
    initializeNavigation();
    initializeAnimations();
    initializeElephantCards();
    initializeScrollEffects();
});

// ===================================
// NAVIGATION
// ===================================
function initializeNavigation() {
    const navLinks = document.querySelectorAll('.nav-link');
    const currentPage = window.location.pathname.split('/').pop() || 'index.html';

    navLinks.forEach(link => {
        const href = link.getAttribute('href');
        if (href === currentPage) {
            link.classList.add('active');
        }

        link.addEventListener('click', (e) => {
            // Allow normal navigation
            navLinks.forEach(l => l.classList.remove('active'));
            link.classList.add('active');
        });
    });

    // Navbar scroll effect
    let lastScroll = 0;
    const navbar = document.querySelector('.navbar');

    window.addEventListener('scroll', () => {
        const currentScroll = window.pageYOffset;

        if (currentScroll > 100) {
            navbar.style.boxShadow = '0 4px 20px rgba(0, 0, 0, 0.3)';
        } else {
            navbar.style.boxShadow = '0 8px 32px 0 rgba(0, 0, 0, 0.37)';
        }

        lastScroll = currentScroll;
    });
}

// ===================================
// ANIMATIONS
// ===================================
function initializeAnimations() {
    // Intersection Observer for fade-in animations
    const observerOptions = {
        threshold: 0.1,
        rootMargin: '0px 0px -50px 0px'
    };

    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                entry.target.style.opacity = '1';
                entry.target.style.transform = 'translateY(0)';
            }
        });
    }, observerOptions);

    // Observe all cards and sections
    const animatedElements = document.querySelectorAll(
        '.overview-card, .finding-card, .feature-card, .elephant-card'
    );

    animatedElements.forEach(el => {
        el.style.opacity = '0';
        el.style.transform = 'translateY(20px)';
        el.style.transition = 'opacity 0.6s ease, transform 0.6s ease';
        observer.observe(el);
    });
}

// ===================================
// ELEPHANT CARDS
// ===================================
function initializeElephantCards() {
    const elephantCards = document.querySelectorAll('.elephant-card');

    elephantCards.forEach(card => {
        card.addEventListener('click', () => {
            const elephant = card.dataset.elephant;
            // Navigate to explorer with elephant pre-selected
            window.location.href = `explorer.html?elephant=${elephant}`;
        });

        // Add hover effect for cursor
        card.style.cursor = 'pointer';
    });
}

// ===================================
// SCROLL EFFECTS
// ===================================
function initializeScrollEffects() {
    // Parallax effect for hero background
    const heroBackground = document.querySelector('.hero-background');

    if (heroBackground) {
        window.addEventListener('scroll', () => {
            const scrolled = window.pageYOffset;
            const rate = scrolled * 0.5;
            heroBackground.style.transform = `translate3d(0, ${rate}px, 0)`;
        });
    }

    // Stat counters animation
    const statValues = document.querySelectorAll('.stat-value');
    let hasAnimated = false;

    const animateStats = () => {
        if (hasAnimated) return;

        const heroSection = document.querySelector('.hero');
        const rect = heroSection.getBoundingClientRect();

        if (rect.top < window.innerHeight && rect.bottom > 0) {
            hasAnimated = true;

            statValues.forEach(stat => {
                const text = stat.textContent;
                const isNumber = /^\d+$/.test(text);

                if (isNumber) {
                    const target = parseInt(text);
                    animateCounter(stat, 0, target, 2000);
                }
            });
        }
    };

    window.addEventListener('scroll', animateStats);
    animateStats(); // Check on load
}

// ===================================
// UTILITY FUNCTIONS
// ===================================
function animateCounter(element, start, end, duration) {
    const range = end - start;
    const increment = range / (duration / 16); // 60fps
    let current = start;

    const timer = setInterval(() => {
        current += increment;
        if (current >= end) {
            element.textContent = end;
            clearInterval(timer);
        } else {
            element.textContent = Math.floor(current);
        }
    }, 16);
}

// ===================================
// SMOOTH SCROLL
// ===================================
document.querySelectorAll('a[href^="#"]').forEach(anchor => {
    anchor.addEventListener('click', function (e) {
        e.preventDefault();
        const target = document.querySelector(this.getAttribute('href'));
        if (target) {
            target.scrollIntoView({
                behavior: 'smooth',
                block: 'start'
            });
        }
    });
});

// ===================================
// EXPORTS
// ===================================
window.elephantPlatform = {
    // Utility functions that can be used across pages
    animateCounter,

    // Behavioral color mapping
    behaviorColors: {
        'Resting': '#3b82f6',
        'Foraging': '#10b981',
        'Movement': '#f59e0b'
    },

    // Elephant metadata
    elephants: {
        'E1': { name: 'Elephant E1', periods: ['pre'], hasPost: false },
        'E2': { name: 'Elephant E2', periods: ['pre'], hasPost: false },
        'E3': { name: 'Elephant E3', periods: ['pre', 'interim', 'post'], hasPost: true },
        'E4': { name: 'Elephant E4', periods: ['pre', 'interim', 'post'], hasPost: true },
        'E5': { name: 'Elephant E5', periods: ['pre', 'interim', 'post'], hasPost: true },
        'E6': { name: 'Elephant E6', periods: ['pre'], hasPost: false }
    },

    // Period metadata
    periods: {
        'pre': { label: 'Pre-Fence Removal', color: '#3b82f6', showFence: true },
        'interim': { label: 'Interim Period', color: '#f59e0b', showFence: true },
        'post': { label: 'Post-Fence Removal', color: '#10b981', showFence: false }
    }
};
